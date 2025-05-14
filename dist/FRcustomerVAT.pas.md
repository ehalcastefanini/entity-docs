<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal:**  
  O código implementa um componente de interface gráfica chamado `TFRAMEcustomerVAT`, que é uma extensão de um frame base (`TFRAMEBaseGridEditSOA`). Ele é utilizado para gerenciar e editar informações relacionadas ao VAT (Imposto sobre Valor Agregado) de clientes. O frame apresenta uma grade (grid) que exibe os dados e permite ações como adicionar, editar e excluir registros.

* **Tecnologias Utilizadas:**  
  - **Delphi:** Linguagem de programação utilizada para criar a aplicação.
  - **Componentes de Interface Gráfica:** `TcxGrid`, `TcxGridDBTableView`, `TsPanel`, `TsBitBtn`.
  - **Serviços SOAP:** Utilização de `SOAPHTTPClient` para comunicação com serviços externos.
  - **Manipulação de Dados:** `DBClient` para manipulação de datasets.

* **Forma do Componente:**  
  - **Exibição em Grade (Grid Display):**
    - **Colunas da Grade e Tipos:**
      - `docCountryDescr` (string): Descrição do país.
      - `consName` (string): Nome do consignatário.
      - `vatPcnt` (float): Percentual do VAT.
      - `vatDescr` (string): Descrição do VAT.
      - `stat` (string): Status.
    - **Ações da Grade e Efeitos:**
      - **Adicionar (`ADD`):** Permite adicionar um novo registro.
      - **Excluir (`DELETE`):** Permite excluir um registro selecionado.

---

## 2. Descrição da Funcionalidade:

* **Ações Disponíveis:**
  - Adicionar um novo registro.
  - Editar valores diretamente na grade.
  - Excluir registros existentes.
  - Pesquisar valores específicos utilizando botões de busca.

* **Componentes Principais:**
  - **Grade (`TcxGrid`):** Exibe os dados em formato tabular.
  - **Botões de Ação:** Botões para adicionar, aplicar, cancelar e excluir ações.
  - **Eventos de Busca:** Implementados para facilitar a pesquisa de valores específicos.

* **Tradução para Pseudo-código:**
  - Evento `OnEditValueChanged` da grade:  
    `if valor da célula alterado then atualizar dataset`.
  - Evento `OnButtonClick` do botão de busca:  
    `if botão clicado then abrir diálogo de busca`.
  - Evento `OnExecute` do botão "Adicionar":  
    `if botão clicado then criar novo registro no dataset`.

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização do frame (`Create`):
     - Configurações da grade, como campos somente leitura, campos ocultos e ordem das colunas.
     - Definição de ações disponíveis (`ADD`, `DELETE`).
     - Atribuição de eventos para botões e campos.
  2. Interação do Usuário:
     - O usuário pode adicionar, editar ou excluir registros diretamente na grade.
     - Ações específicas, como busca, são disparadas por eventos associados a botões.

* **Dados Necessários:**
  - Informações do cliente, como código do país, consignatário e código VAT.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - **Adicionar:** Disponível sempre.
  - **Excluir:** Disponível apenas se um registro estiver selecionado.
  - **Editar:** Permitido apenas se o modo de acesso não for "VIEW" ou "DELETE".

* **Filtros Disponíveis:**
  - Não há filtros explícitos definidos no código.

* **Mensagens de Erro:**
  - Não há mensagens de erro explícitas definidas no código.

* **Valores Padrão dos Campos:**
  - Não há valores padrão explícitos definidos no código.

* **Validações e Condições dos Campos:**
  - Campos como `docCountry`, `cons` e `vatCode` possuem editores personalizados para busca.

---

## 5. Funções Principais:

* **`Create`:** Configura o frame, define propriedades e eventos.
* **`m_GenericFind`:** Realiza buscas genéricas com base no botão clicado.
* **`m_SetAccessMode`:** Define o modo de acesso e permissões de edição.
* **`cxDBVtableEditValueChanged`:** Atualiza o dataset quando um valor na grade é alterado.

---

## 6. Consumo de Serviços API:

* **Chamadas a Serviços Externos:**
  - **Nome do Serviço:** Não especificado.
  - **Endpoint:** Não especificado.
  - **Dados Enviados:** Não especificado.
  - **Dados Recebidos:** Não especificado.
  - **Propósito:** Não especificado.
  - **Tratamento de Erros:** Não especificado.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explícitos definidos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `SOAPHTTPClient`: Para comunicação com serviços SOAP.
  - `cxGrid`, `cxGridDBTableView`: Para exibição de dados em grade.
  - `DBClient`: Para manipulação de datasets.

* **Componentes Personalizados:**
  - `TFRAMEBaseGridEditSOA`: Frame base herdado.
  - `kneFRGridManager`: Gerenciador de grades.

---

## 9. Listagem de Campos e Validações:

* **Campos:**
  - `docCountryDescr` (string, somente leitura).
  - `consName` (string, somente leitura).
  - `vatPcnt` (float, somente leitura).
  - `vatDescr` (string, somente leitura).
  - `stat` (string, somente leitura).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - `docCountryDescr` → `docCountry`.
  - `consName` → `cons`.
  - `vatPcnt` → `vatCode`.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:**  
  Não aplicável.

* **Diagrama de Sequência:**  
  Não aplicável.

* **Trechos de Código:**  
  ```delphi
  procedure TFRAMEcustomerVAT.cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView; AItem: TcxCustomGridTableItem);
  begin
    // Atualiza o dataset quando o valor da célula é alterado
  end;
  ```

* **HTML Representando a Grade:**  
  ```html
  <table style="border: 1px solid black; width: 100%;">
    <thead>
      <tr>
        <th>Descrição do País</th>
        <th>Nome do Consignatário</th>
        <th>Percentual VAT</th>
        <th>Descrição VAT</th>
        <th>Status</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td>Portugal</td>
        <td>Empresa A</td>
        <td>23%</td>
        <td>IVA Padrão</td>
        <td>Ativo</td>
      </tr>
    </tbody>
  </table>
  ```

---

## 11. Comentários Importantes no Código:

* **Configuração do Frame:**
  ```delphi
  MasterKeyFields := 'customerCode=cust';
  DataPacketName := 'CustVatExcept';
  PropertyName := 'custVatExcept';
  FrameType := frtDetail;
  ```

* **Configuração da Grade:**
  ```delphi
  DefineReadOnlyFields('docCountryDescr; consName; vatPcnt; vatDescr; stat');
  DefineHiddenFields('HIDE_ALL_FIELDS');
  DefineOrderFields('docCountry; docCountryDescr; cons; consName; vatCode; vatPcnt; vatDescr');
  ```

---

## 12. Conclusão:

O código implementa um frame funcional para gerenciar informações de VAT de clientes, com suporte a ações básicas como adicionar, editar e excluir registros. Ele é bem estruturado, mas carece de validações explícitas e mensagens de erro. Além disso, a integração com serviços externos não está detalhada.

---

## 13. Resumo Curto:

O `TFRAMEcustomerVAT` é um frame para gerenciar dados de VAT de clientes, exibindo informações em uma grade e permitindo ações como adicionar, editar e excluir registros. Ele utiliza componentes visuais e serviços SOAP para manipulação de dados.#### **FRcustomerVAT.pas**

```
unit FRcustomerVAT;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, sFrameAdapter, ImgList, ActnList, ExtCtrls, Rio,
  SOAPHTTPClient, DBClient, kneFRGridManager, StdCtrls, Buttons, sBitBtn,
  sPanel, cxGridLevel, cxClasses, cxControls, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid,
  kneTypes, cxButtonEdit{m_GenericFind};

type
  TFRAMEcustomerVAT = class(TFRAMEBaseGridEditSOA)
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure ACTaddExecute(Sender: TObject);
    procedure CDStableNewRecord(DataSet: TDataSet);
  private
    { Private declarations }
    procedure m_GenericFind(Sender: TObject;AButtonIndex: Integer);
    procedure m_SetAccessMode(Sender: TObject; var pv_State: Boolean);
    procedure m_FindByCode(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_SetFindCountry(pv_FieldName: string; Sender: TObject);
    procedure m_SetFindCons(pv_FieldName: string; Sender: TObject);
    procedure m_SetFindVatCode(pv_FieldName: string; Sender: TObject);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

var
  FRAMEcustomerVAT: TFRAMEcustomerVAT;

implementation

uses
  kneFindDialog, kneFGFindUtils, kneDialogFactory, kneUtils, Global,
  BaseServiceUtils, CountryServiceUtils, ConsigneeServiceUtils,
  VatCodeServiceUtils;


{$R *.dfm}

{ TFRAMEcustomerVAT }

constructor TFRAMEcustomerVAT.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'customerCode=cust';
  DataPacketName := 'CustVatExcept';    // o nome do detail no datapacket(metadata) � sempre no singular
  PropertyName := 'custVatExcept';      // nome do campo da metadata que vai conter os details
  FrameType := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  AvailableActions := 'ADD;DELETE';

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Read-Only ........................................................
    DefineReadOnlyFields('docCountryDescr; consName; vatPcnt; vatDescr; stat');
    // Campos Hidden ...........................................................
    DefineHiddenFields('HIDE_ALL_FIELDS');
    // Ordem Campos ............................................................
    DefineOrderFields('docCountry; docCountryDescr; cons; consName;' +
      'vatCode; vatPcnt; vatDescr');

    // Key Fields ..............................................................
    KeyFields:= 'cust;docCountry;cons';

    // Custom Editors ..........................................................
    AddCustomField('docCountry','cxEDTfind');
    AddCustomField('cons','cxEDTfind');
    AddCustomField('vatCode','cxEDTfind');
  end; //with

  // Atribui��o dos eventos dos Finds
  cxEDTfind.Properties.OnButtonClick := m_GenericFind;

  //
  OnSetAccessMode := m_SetAccessMode;

end;


procedure TFRAMEcustomerVAT.m_SetAccessMode(Sender: TObject;
  var pv_State: Boolean);
begin
	if (UpperCase(AccessMode) <> 'VIEW') and (UpperCase(AccessMode) <> 'DELETE') then
 	begin
    pv_State := False; // impedir execu��o do c�digo da base
 		SetKeyEditing(True); // permitir edi��o dos campos que fazem parte da chave
```

#### **FRcustomerVAT.dfm**

```
inherited FRAMEcustomerVAT: TFRAMEcustomerVAT
  ParentFont = True
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
    end
  end
  inherited PNLfooter: TsPanel
    inherited PNLeditActions: TsPanel
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

