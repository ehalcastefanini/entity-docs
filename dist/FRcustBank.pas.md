<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código apresentado implementa um componente de interface gráfica chamado `TFRAMEcustBank`, que é uma extensão de um frame base para edição de grids (`TFRAMEBaseGridEditSOA`). Ele é utilizado para gerenciar informações bancárias de clientes em um sistema. O objetivo principal é permitir a visualização, edição e manipulação de dados bancários associados a clientes, como códigos de banco, nomes de banco, status, entre outros.

* **Tecnologias Utilizadas:**
  - **Delphi:** Linguagem de programação utilizada para desenvolver o componente.
  - **Bibliotecas e Componentes:**
    - `cxGrid`, `cxGridDBTableView`: Componentes para exibição de grids de dados.
    - `SOAPHTTPClient`: Para comunicação com serviços SOAP.
    - `DBClient`: Para manipulação de datasets.
    - `kneFRGridEditSOA`: Frame base para edição de grids.
    - `kneUtils`, `kneTypes`, `kneConfigObjects`: Utilitários e tipos personalizados.
    - `BankServiceUtils`: Utilitário específico para manipulação de dados bancários.

* **Forma do Componente:**
  - **Grid Display:**
    - **Colunas do Grid e seus Tipos:**
      - `mill`: Identificador do cliente (string).
      - `bankCode`: Código do banco (string).
      - `bankName`: Nome do banco (string).
      - `stat`: Status do banco (string).
      - `lastUpd`: Última atualização (data/hora).
      - `updBy`: Usuário que realizou a última atualização (string).
    - **Ações do Grid e seus Efeitos:**
      - Edição de valores diretamente no grid.
      - Busca de bancos por código.
      - Validação de campos ao alterar valores.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Permitir a edição de informações bancárias diretamente no grid.
  - Buscar bancos por código utilizando um diálogo de busca.
  - Configurar campos como somente leitura ou ocultos.

* **Componentes Principais:**
  - `cxGridDBTableView`: Exibe os dados em formato de tabela.
  - `m_SetFindEditBank`: Configura o diálogo de busca para seleção de bancos.
  - `GridSettings`: Configurações do grid, como campos ocultos, ordem de exibição e editores personalizados.

* **Pseudo-código de Ações e Eventos:**
  - Evento `OnEditValueChanged`:
    ```pseudo
    se valor de um campo no grid for alterado então
        validar e processar a alteração
    ```
  - Método `m_SetFindEditBank`:
    ```pseudo
    se botão de busca for clicado então
        abrir diálogo de busca
        configurar campos de busca e seleção
    ```
  - Método `Create`:
    ```pseudo
    ao criar o frame então
        configurar propriedades do frame
        definir configurações do grid
    ```

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização do frame (`Create`):
     - Configura propriedades como `MasterKeyFields`, `DataPacketName`, e `FrameType`.
     - Define configurações do grid, como campos ocultos, ordem de exibição e editores personalizados.
  2. Interação do Usuário:
     - O usuário pode editar valores diretamente no grid.
     - O botão de busca permite abrir um diálogo para selecionar bancos.
  3. Eventos:
     - Alterações nos valores do grid disparam o evento `OnEditValueChanged`.

* **Dados Necessários:**
  - Código do cliente (`customerCode`).
  - Informações bancárias, como código do banco, nome do banco e status.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - Ação: Alterar valores no grid.
    - Pré-condição: O campo deve estar habilitado para edição.
  - Ação: Buscar banco por código.
    - Pré-condição: O botão de busca deve ser clicado.

* **Filtros Disponíveis:**
  - Não há filtros explícitos definidos no código.

* **Mensagens de Erro:**
  - Não há mensagens de erro explícitas definidas no código.

* **Valores Padrão dos Campos:**
  - Não há valores padrão explícitos definidos no código.

* **Validações e Condições dos Campos:**
  - Validações específicas não estão definidas no código.

---

## 5. Funções Principais:

* **`Create`:**
  - Configura o frame e define as propriedades e configurações do grid.
* **`m_SetFindEditBank`:**
  - Configura e exibe o diálogo de busca para seleção de bancos.
* **`cxDBVtableEditValueChanged`:**
  - Processa alterações nos valores do grid.

---

## 6. Consumo de Serviços API:

* **Serviço Externo:**
  - Nome do Serviço: Não especificado.
  - Endpoint: Não especificado.
  - Dados Enviados: Não especificado.
  - Dados Recebidos: Não especificado.
  - Propósito: Não especificado.
  - Tratamento de Erros: Não especificado.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explícitos definidos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `cxGrid`, `cxGridDBTableView`: Para exibição de grids.
  - `SOAPHTTPClient`: Para comunicação com serviços SOAP.
  - `DBClient`: Para manipulação de datasets.

* **Componentes Personalizados:**
  - `kneFRGridEditSOA`: Frame base para edição de grids.
  - `BankServiceUtils`: Utilitário para manipulação de dados bancários.

---

## 9. Listagem de Campos e Validações:

* **Campos no Grid:**
  - `mill` (tipo: string, não definido como obrigatório).
  - `bankCode` (tipo: string, não definido como obrigatório).
  - `bankName` (tipo: string, não definido como obrigatório).
  - `stat` (tipo: string, não definido como obrigatório).
  - `lastUpd` (tipo: data/hora, não definido como obrigatório).
  - `updBy` (tipo: string, não definido como obrigatório).

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - Não especificado no código.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Exemplo de Código:**
  ```delphi
  FRAMEcustBank := TFRAMEcustBank.Create(Self);
  FRAMEcustBank.SetForDOCADDR;
  ```
* **HTML Representando o Grid:**
  ```html
  <table style="border: 1px solid black; width: 100%;">
    <thead>
      <tr>
        <th>mill</th>
        <th>bankCode</th>
        <th>bankName</th>
        <th>stat</th>
        <th>lastUpd</th>
        <th>updBy</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td>001</td>
        <td>123</td>
        <td>Banco A</td>
        <td>Ativo</td>
        <td>2023-10-01</td>
        <td>Admin</td>
      </tr>
    </tbody>
  </table>
  ```

---

## 11. Comentários Importantes no Código:

* Configuração de propriedades do frame no método `Create`.
* Configuração de campos ocultos e ordem de exibição no grid.

---

## 12. Conclusão:

O código implementa um frame para gerenciar informações bancárias de clientes, com funcionalidades de edição e busca. Ele é bem estruturado, mas carece de validações explícitas e mensagens de erro. Sua força está na flexibilidade de configuração do grid.

---

## 13. Resumo Curto:

O `TFRAMEcustBank` é um frame para gerenciar dados bancários de clientes, permitindo edição e busca. Ele utiliza grids configuráveis e é extensível, mas não define validações ou mensagens de erro explícitas.#### **FRcustBank.pas**

```
unit FRcustBank;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, sFrameAdapter, ImgList, ActnList, ExtCtrls, Rio,
  SOAPHTTPClient, DBClient, kneFRGridManager, StdCtrls, Buttons, sBitBtn,
  sPanel, cxGridLevel, cxClasses, cxControls, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid;

type
  TFRAMEcustBank = class(TFRAMEBaseGridEditSOA)
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
  private
    { Private declarations }
    procedure m_FindByCodeBank(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_SetFindEditBank(Sender: TObject; AButtonIndex: Integer);
    procedure SetKeyEditing(const EditKey: Boolean);  override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;

    procedure SetForDOCADDR;
  end;

var
  FRAMEcustBank: TFRAMEcustBank;

implementation

{$R *.dfm}

uses
  kneUtils, kneTypes, kneConfigObjects, kneFindDialog, kneDialogFactory,
  kneFGFindUtils, Global,
  //---
  BankServiceUtils;

{ TFRAMEcustBank }

constructor TFRAMEcustBank.Create(AOwner: TComponent);
begin
  inherited;
  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'customerCode=customer';
  DataPacketName := 'CustomerBank';
  PropertyName := 'banks';
  FrameType := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';
  PNLfooter.Visible := False;
    
  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Read-Only ........................................................
    // Campos Hidden ...........................................................
    DefineHiddenFields('HIDE_ALL_FIELDS');
    // Ordem Campos ............................................................
    DefineOrderFields('mill; bankCode; bankName; stat; lastUpd; updBy');
    // Key Fields ..............................................................
    KeyFields:= 'customer;mill;bankCode';
    // Custom Editors ..........................................................
    AddCustomField('bankCode','cxEDTfind');
    AddCustomField('stat','cxEDTstat');
  end; //with

  cxEDTfind.Properties.OnButtonClick := m_SetFindEditBank;
end;

procedure TFRAMEcustBank.m_SetFindEditBank(Sender: TObject;
  AButtonIndex: Integer);
var
  lv_Find: TFORMkneFindDialog;
begin

  try    // inicializa��o de Find Dialog
    lv_Find := nil;
    lv_Find := TkneDialogFactory.GetFindDialog(Application);

    with lv_Find.Options.DataSelection do
    begin
      // campos para selec��o do Find DataSet
      FieldNameForCode:= 'bankCode';
      DefineFieldsForDesc('name; stat;');

      TargetDataSet:= CDStable;
      TargetFieldNameForCode:= 'bankCode';
      DefineTargetFieldsForDesc('bankName; stat;');

      UseTargetDataSet:= True;
```

#### **FRcustBank.dfm**

```
inherited FRAMEcustBank: TFRAMEcustBank
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

