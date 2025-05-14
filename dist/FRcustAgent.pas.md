<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal e Problema Resolvido:**
  O código implementa um componente de interface gráfica para gerenciar agentes de clientes em um sistema. Ele fornece uma interface de grade (grid) para exibir, editar e gerenciar informações relacionadas a agentes de clientes, como código do agente, nome, taxa de comissão, tipo de comissão, entre outros. O objetivo é facilitar a manipulação e visualização de dados relacionados a agentes de clientes.

* **Tecnologias Utilizadas:**
  - **Delphi:** Linguagem de programação utilizada para desenvolver a aplicação.
  - **Componentes cxGrid:** Utilizados para criar e gerenciar a interface de grade.
  - **SOAP (Simple Object Access Protocol):** Para comunicação com serviços externos.
  - **Bibliotecas Personalizadas:** Como `kneUtils`, `kneTypes`, `kneConfigObjects`, entre outras.

* **Forma do Componente:**
  - **Exibição em Grade (Grid Display):**
    - **Colunas da Grade e seus Tipos:**
      - `mill` (string): Código do moinho.
      - `agentCode` (string): Código do agente.
      - `agentName` (string): Nome do agente.
      - `commRate` (float): Taxa de comissão.
      - `commType` (string): Tipo de comissão.
      - `stat` (string): Status.
      - `lastUpd` (datetime): Última atualização.
      - `updBy` (string): Atualizado por.
    - **Ações da Grade e seus Efeitos:**
      - Edição de valores diretamente na grade.
      - Busca de agentes por código ou nome.
      - Configuração de campos customizados e validações.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**
  - Permitir a edição de valores diretamente na grade.
  - Configurar campos customizados para busca e validação.
  - Gerenciar a visibilidade e ordem das colunas da grade.
  - Buscar agentes por código ou nome.

* **Componentes Principais:**
  - `TFRAMEcustAgent`: Classe principal que gerencia a interface e lógica da grade.
  - `cxGrid`: Componente de grade para exibição de dados.
  - `cxDBVtable`: Visualização da tabela de dados vinculada ao banco de dados.

* **Tradução para Pseudo-código:**
  - Evento `OnEditValueChanged`:
    ```pseudo
    se valor do campo na grade for alterado então
      executar validação ou lógica associada ao campo
    ```
  - Botão de busca:
    ```pseudo
    se botão de busca for clicado então
      abrir diálogo de busca
      preencher campo com o valor selecionado
    ```

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**
  1. Inicialização do componente `TFRAMEcustAgent`.
  2. Configuração das propriedades da grade, como campos visíveis, ordem, e campos customizados.
  3. Interação do usuário com a grade (edição, busca, etc.).
  4. Execução de eventos associados, como validação de campos ou busca de dados.

* **Dados Necessários:**
  - Código do cliente (`customerCode`).
  - Informações do agente (código, nome, tipo de comissão, etc.).

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**
  - A edição de valores na grade só é permitida para campos não marcados como "Read-Only".
  - A busca de agentes requer que o usuário clique no botão de busca.

* **Filtros Disponíveis:**
  - Não há filtros explícitos definidos no código.

* **Mensagens de Erro:**
  - Não há mensagens de erro explícitas definidas no código.

* **Valores Padrão dos Campos:**
  - Não há valores padrão explícitos definidos no código.

* **Validações e Condições dos Campos:**
  - Campos customizados possuem validações específicas, como `cxEDTfind` para busca e `cxICBcommType` para tipo de comissão.

---

## 5. Funções Principais:

* **`Create`:** Inicializa o componente e configura as propriedades da grade.
* **`m_SetFindEditAgent`:** Configura o diálogo de busca para selecionar agentes.
* **`SetFieldValuesToCDS`:** Define valores de campos no conjunto de dados.

---

## 6. Consumo de Serviços API:

* **Chamadas a Serviços Externos:**
  - Serviço: `AgentServiceUtils`.
  - Propósito: Não especificado no código.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explícitos definidos no código.

---

## 8. Dependências:

* **Bibliotecas Externas:**
  - `cxGrid`, `cxDBData`: Para exibição e manipulação de dados em grade.
  - `SOAPHTTPClient`: Para comunicação com serviços SOAP.

* **Componentes Customizados:**
  - `kneUtils`, `kneTypes`, `kneConfigObjects`: Utilizados para lógica e configurações específicas.

---

## 9. Listagem de Campos e Validações:

* **Campos:**
  - `mill` (string): Não definido no código.
  - `agentCode` (string): Não definido no código.
  - `agentName` (string): Não definido no código.
  - `commRate` (float): Não definido no código.
  - `commType` (string): Não definido no código.
  - `stat` (string): Não definido no código.
  - `lastUpd` (datetime): Não definido no código.
  - `updBy` (string): Não definido no código.

* **Mapeamento de Valores e Colunas do Banco de Dados:**
  - Não especificado no código.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:** Não aplicável.
* **Diagrama de Sequência:** Não aplicável.
* **Exemplo de Código:**
  ```delphi
  FRAMEcustAgent := TFRAMEcustAgent.Create(Self);
  FRAMEcustAgent.Parent := Self;
  ```
* **HTML Renderizado:**
  ```html
  <table style="width:100%; border:1px solid black;">
    <tr>
      <th>Mill</th>
      <th>Agent Code</th>
      <th>Agent Name</th>
      <th>Comm Rate</th>
      <th>Comm Type</th>
      <th>Status</th>
      <th>Last Update</th>
      <th>Updated By</th>
    </tr>
    <tr>
      <td>001</td>
      <td>A123</td>
      <td>John Doe</td>
      <td>5%</td>
      <td>Type A</td>
      <td>Active</td>
      <td>2023-10-01</td>
      <td>Admin</td>
    </tr>
  </table>
  ```

---

## 11. Comentários Importantes no Código:

* Configuração de propriedades da grade, como campos visíveis, ordem e validações.
* Uso de métodos customizados para busca e validação de dados.

---

## 12. Conclusão:

O código fornece uma interface robusta para gerenciar agentes de clientes, com funcionalidades de edição, busca e validação. No entanto, faltam definições explícitas de validações e mensagens de erro, o que pode limitar a usabilidade em cenários mais complexos.

---

## 13. Resumo Curto:

O código implementa uma interface de grade para gerenciar agentes de clientes, permitindo edição, busca e validação de dados. Ele utiliza componentes visuais e bibliotecas personalizadas para oferecer uma solução eficiente e configurável.#### **FRcustAgent.pas**

```
unit FRcustAgent;

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
  TFRAMEcustAgent = class(TFRAMEBaseGridEditSOA)
    cxICBcommType: TcxEditRepositoryImageComboBoxItem;
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
  private
    { Private declarations }
    procedure m_SetFindEditAgent(Sender: TObject;
      AButtonIndex: Integer);
    procedure m_FindByCodeAgent(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure SetKeyEditing(const EditKey: Boolean);  override;
    function SetFieldValuesToCDS(const pv_dataset: TDataset; 
      const pv_fieldsValues: string; pv_Separador: String = '|'): string;  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;

    procedure SetForDOCADDR;
  end;

var
  FRAMEcustAgent: TFRAMEcustAgent;

implementation

{$R *.dfm}

uses
  kneUtils, kneTypes, kneConfigObjects, kneFindDialog, kneDialogFactory,
  kneFGFindUtils, Global,
  AgentServiceUtils;

{ TFRAMEcustAgent }

constructor TFRAMEcustAgent.Create(AOwner: TComponent);
begin
  inherited;
  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'customerCode=customer';
  DataPacketName := 'CustomerAgent';
  PropertyName := 'agents';
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
    DefineOrderFields('mill; agentCode; agentName; commRate; commType; stat; lastUpd; updBy');
    // Key Fields ..............................................................
    KeyFields:= 'customer;mill;agentCode';
    // Custom Editors ..........................................................
    AddCustomField('agentCode','cxEDTfind');
    AddCustomField('commType','cxICBcommType');
    AddCustomField('stat','cxEDTstat');
  end; //with

  ColsWidthInGrid := '45;70;200;55;55;95;105;70';
  cxEDTfind.Properties.OnButtonClick := m_SetFindEditAgent;
end;


procedure TFRAMEcustAgent.m_SetFindEditAgent(Sender: TObject;
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
      FieldNameForCode:= 'Code';
      FieldNamesForDesc.Clear;
      FieldNamesForDesc.Add('name');
```

#### **FRcustAgent.dfm**

```
inherited FRAMEcustAgent: TFRAMEcustAgent
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

