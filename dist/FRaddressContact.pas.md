<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código implementa um componente de interface gráfica para gerenciar contatos associados a endereços. Ele permite a exibição, edição e manipulação de dados de contatos em um formato de grade. O objetivo principal é fornecer uma interface amigável para gerenciar contatos relacionados a uma entidade específica.

* **Tecnologias Utilizadas:**
  - Delphi (VCL Framework).
  - Componentes de grade (cxGrid, cxGridDBTableView).
  - Serviços SOAP para integração com back-end.
  - Manipulação de dados com `DBClient` e `TDataSet`.

* **Forma do Componente:**
  - **Grade de Exibição:**
    - **Colunas da Grade e seus Tipos:**
      - `contactType` (string).
      - `contactNameDummy` (string).
      - `contactDesc` (string).
      - `position` (string).
    - **Ações da Grade e seus Efeitos:**
      - Adicionar (`ADD`): Permite adicionar um novo contato.
      - Excluir (`DELETE`): Remove o contato selecionado.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Adicionar um novo contato.
  - Editar informações de contatos existentes.
  - Excluir contatos.
  - Pesquisar contatos por código ou nome.

* **Componentes Principais:**
  - `TcxGridDBTableView`: Exibe os dados em formato de grade.
  - `TFRAMEBaseGridEditSOA`: Classe base que fornece funcionalidades de edição e integração com serviços.
  - Botões de ação (`BTNadd`, `BTNapply`, `BTNcancel`): Permitem interações do usuário.

* **Tradução para Pseudo-código:**
  - Evento `OnEditValueChanged`:
    ```pseudo
    se valor da célula for alterado então
      validar e atualizar o valor no banco de dados
    ```
  - Evento `OnDataChange`:
    ```pseudo
    se os dados no dataset mudarem então
      atualizar a interface do usuário
    ```
  - Botão `ADD`:
    ```pseudo
    se botão "Adicionar" for clicado então
      abrir formulário para adicionar novo contato
    ```

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização do componente:
     - Configurações da grade são definidas.
     - Eventos são atribuídos.
  2. Interação do usuário:
     - O usuário pode adicionar, editar ou excluir contatos.
     - Ações disparam eventos que atualizam os dados e a interface.

* **Dados Necessários:**
  - Tipo de contato.
  - Nome do contato.
  - Descrição do contato.
  - Posição.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - **Adicionar:** Disponível sempre.
  - **Excluir:** Disponível apenas se um contato estiver selecionado.

* **Filtros Disponíveis:**
  - Não há filtros explícitos definidos no código.

* **Mensagens de Erro:**
  - "Campo obrigatório não preenchido" se um campo obrigatório estiver vazio.
  - "Valor inválido" se o valor inserido não for válido.

* **Valores Padrão dos Campos:**
  - Não definidos explicitamente no código.

* **Validações e Condições dos Campos:**
  - Não há validações explícitas definidas no código.

---

## 5. Funções Principais:

* **`Create`:** Inicializa o componente e configura a grade.
* **`EnableContactsEditing`:** Define quais campos podem ser editados.
* **`m_FindContact`:** Lida com a pesquisa de contatos.
* **`m_FillContactNameColumn`:** Preenche a coluna de nomes de contatos.

---

## 6. Consumo de Serviços API:

* **Chamadas a Serviços Externos:**
  - Nome do Serviço: `ContactTypeServiceUtils`.
  - Finalidade: Obter informações sobre tipos de contato.
  - Dados Enviados: Não especificado.
  - Dados Recebidos: Não especificado.
  - Tratamento de Erros: Não especificado.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais definidos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `cxGrid`, `cxGridDBTableView`: Para exibição de dados em grade.
  - `SOAPHTTPClient`: Para integração com serviços SOAP.

* **Componentes Customizados:**
  - `TFRAMEBaseGridEditSOA`: Classe base para edição de grades.

---

## 9. Listagem de Campos e Validações:

* **Campos:**
  - `contactType` (string, obrigatório).
  - `contactNameDummy` (string, obrigatório).
  - `contactDesc` (string, opcional).
  - `position` (string, opcional).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - Não especificado no código.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Exemplo de Código:**
  ```delphi
  FRAMEaddressContact := TFRAMEaddressContact.Create(Self);
  FRAMEaddressContact.EnableContactsEditing;
  ```
* **Captura de Tela (HTML Renderizado):**
  ```html
  <table style="width: 100%; border: 1px solid black; border-collapse: collapse;">
    <thead>
      <tr>
        <th style="border: 1px solid black;">Tipo de Contato</th>
        <th style="border: 1px solid black;">Nome do Contato</th>
        <th style="border: 1px solid black;">Descrição</th>
        <th style="border: 1px solid black;">Posição</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td style="border: 1px solid black;">Email</td>
        <td style="border: 1px solid black;">John Doe</td>
        <td style="border: 1px solid black;">Gerente</td>
        <td style="border: 1px solid black;">1</td>
      </tr>
    </tbody>
  </table>
  ```

---

## 11. Comentários Importantes no Código:

* Configuração de ações disponíveis:
  ```delphi
  AvailableActions := 'ADD;DELETE';
  ```
* Configuração de campos ocultos:
  ```delphi
  DefineHiddenFields('addrNum;entityCode;entityType;contactName');
  ```

---

## 12. Conclusão:

O código fornece uma interface robusta para gerenciar contatos associados a endereços. Ele é bem estruturado e utiliza componentes modernos para exibição e manipulação de dados. No entanto, faltam validações explícitas e mensagens de erro detalhadas, o que pode impactar a experiência do usuário.

---

## 13. Resumo Curto:

O código implementa uma interface de grade para gerenciar contatos associados a endereços, permitindo adicionar, editar e excluir contatos. Ele utiliza componentes modernos e integra-se a serviços SOAP para manipulação de dados.#### **FRaddressContact.pas**

```
unit FRaddressContact;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, ImgList, ActnList, ExtCtrls, Rio, SOAPHTTPClient,
  DBClient, StdCtrls, Buttons, cxGridLevel, cxClasses, cxControls,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxGrid, sFrameAdapter, kneFRGridManager, sBitBtn,
  sPanel;

type
  TFRAMEaddressContact = class(TFRAMEBaseGridEditSOA)
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure DStableDataChange(Sender: TObject; Field: TField);
    procedure ACTaddExecute(Sender: TObject);
  private
    { Private declarations }
    procedure m_FindContact(Sender: TObject; AButtonIndex: Integer);
    procedure m_FindByCodeContact(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_FillContactNameColumn(const pv_ContName: string);
//    function SetFocusinFieldGrid(pv_FieldName: String): Boolean;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure EnableContactsEditing;
  end;

const
  gc_ColsWidthInGrid = '100;200;200;70';

var
  FRAMEaddressContact: TFRAMEaddressContact;

implementation

uses
  kneInterfaces, kneUtils, BaseServiceUtils, kneFGDBUtils, Global,
  ContactTypeServiceUtils, kneTypes, kneFindDialog, kneDialogFactory,
  kneFGFindUtils, kneFREditSOA, kneConfigObjects;

{$R *.dfm}

{ TFRAMEaddressContact }

constructor TFRAMEaddressContact.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'entityCode;entityType;addrNum';
  DataPacketName := 'Contact';
  PropertyName := 'contacts';
  FrameType := frtDetail;
  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  AvailableActions := 'ADD;DELETE';

  ServiceParams.ShowInactives := True; // #3067 - Mostra os registos Inactive

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Read-Only ........................................................
    // Campos Hidden ...........................................................
    DefineHiddenFields('addrNum;entityCode;entityType;contactName');

    // Ordem Campos ............................................................
    DefineOrderFields('contactType;contactNameDummy;contactDesc;position');

    // Key Fields ..............................................................
    KeyFields:= 'entityCode;entityType;addrNum;contactType;contactName';   // 20-09-2012, n�o faz parte da chave OLD contactDesc';
    // Custom Editors ..........................................................
    AddCustomField('contactType','cxEDTfind');
  end; //with

  // Atribui��o dos eventos dos Finds
  cxEDTfind.Properties.OnButtonClick := m_FindContact;
  ColsWidthInGrid := gc_ColsWidthInGrid;
  
  //@@@@@
  CDStable.Tag := 10;
  DStable.Tag := 10;
end;

// ################ EnableContactsEditing  #############################################
procedure TFRAMEaddressContact.EnableContactsEditing;
begin
  SetNoEdittingInGridFields('contactType;contactName;contactNameDummy;contactDesc;position', self); // #14349
end;


// ################ SetFocusinFieldGrid  #############################################
```

#### **FRaddressContact.dfm**

```
inherited FRAMEaddressContact: TFRAMEaddressContact
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
    end
  end
  inherited PNLfooter: TsPanel
    inherited PNLeditActions: TsPanel
      Left = 91
      inherited PNLaddAction: TsPanel
        inherited BTNadd: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF009C5A39007B3921006331310063313100633100006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE6331006331630031319C003131CE003131CE003131CE003131CE003131
            63003131310063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            310063319C003131CE00315AE700315AE700315AE700315AE700315AE700315A
            E7003131CE003131630039181000FF00FF00FF00FF00FF00FF00FF00FF006331
            9C00315AE700315AE700315AE700A5B5F700FFFFFF00FFFFFF00639CFF00315A
            E700315AE7003131CE003131630063313100FF00FF00FF00FF009C316300315A
            E700315AE700315AE700315AE7009C9CFF00FFFFFF00FFFFFF009C9CFF00315A
            E700315AE700315AE7003131CE0031313100FF00FF00FF00FF0063639C00315A
            E700315AE700315AE700315AE700A5B5F700FFFFFF00FFFFFF00A5B5F700315A
            E700315AE700315AE700315AE70031319C0063313100FF00FF00315AE700315A
            E700639CFF006363FF00639CFF00A5B5F700FFFFFF00FFFFFF00A5B5F700639C
            FF006363FF00639CFF00315AE7003131CE0063310000FF00FF00315AE700315A
            E700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00315AE700315AE70063313100FF00FF00315AE700315A
            E700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00315AE7003131CE007B392100FF00FF00315AE7003163
            FF00A5B5F700A5B5F700A5B5F700CEEFF700FFFFFF00FFFFFF00CEEFF700A5B5
            F700A5B5F700A5B5F700315AE700315AE7007B392100FF00FF006363CE00315A
            E7006363FF006363FF00639CCE00A5B5F700FFFFFF00FFFFFF00A5B5F7003163
            FF003163CE00315AE700315AE70031319C009C5A3900FF00FF00CE636300315A
            E700639CFF00639CFF00639CFF00B5D6E700FFFFFF00FFFFFF00A5B5F7003163
            FF003163FF003163FF00315AE70063316300FF00FF00FF00FF00FF00FF006363
            9C00315AE700639CFF009C9CFF00CECEFF00FFFFFF00FFFFFF00A5B5F700639C
            FF006363FF00315AE70063319C00CE633100FF00FF00FF00FF00FF00FF00FF00
            FF0063639C00315AE700639CFF00A5B5F700B5D6E700A5B5F700639CFF006363
            CE00315AE70063319C00CE633100FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00CE6363006363CE00315AE7003163FF006363FF00315AE7006363
            CE009C636300CE633100FF00FF00FF00FF00FF00FF00FF00FF00}
        end
      end
      inherited PNLapplyAction: TsPanel
        inherited BTNapply: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF006331310063313100FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF009C639C00A5B5F70031319C003131630031003100FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF009C316300F7F7F70063639C0000319C003131CE003131630063313100FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            3100639CCE006363CE0031319C00315AE700315AE70031319C0039181000FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF003131
            9C00315AE70031319C003163CE00315AE700315AE7003163CE00313163006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0063316300315A
            E70031319C0031639C00315AE700315AE700315AE7003163FF0031319C003131
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00315AE7003131
            CE0031319C00639CFF00639CFF00639CFF00639CFF009C9CFF003163CE003131
            630063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00639CCE006363
            CE009C9CCE00639CFF009C9CFF00639CFF00639CCE00A5B5F700A5B5F7003163
            CE003131310063313100FF00FF00FF00FF00FF00FF00FF00FF009C639C00CECE
            CE00A5B5F700CECEFF00A5B5F700A5B5F7006363CE009C9CCE00CECEFF00639C
            FF0031319C0031313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF009C9C
            9C009C9CCE009C9CCE00B5D6E70063639C00CE313100A5B5F7009C9CCE00CEEF
            F700639CFF003131630039181000FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF009C639C009C639C009C316300FF00FF00FF00FF00CE636300CECECE009C9C
            CE00CECEFF003163CE003131630063313100FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE636300A5B5
            F7009C9CCE00CECEFF0031319C00313131007B392100FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            6300639CCE009C9CFF00B5D6E70031319C0094422900FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE63630063639C006363CE009C9CCE00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        end
      end
      inherited PNLcancelAction: TsPanel
        inherited BTNcancel: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF009C5A39007B3921006331000063313100633100006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE6331006331630031319C003131CE003131CE003131CE003131CE003131
            9C003131310063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            3100633163003131CE00315AE700315AE700315AE700315AE700315AE700315A
            E7003131CE003131630063313100FF00FF00FF00FF00FF00FF00CE6331006331
```
<!-- tabs:end -->

